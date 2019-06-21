#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class bnk_recon_reg : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "bnk_recon_reg.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();

            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = PrmComp;
            if (aUsers == "external")
            {

            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }


            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'bnk_recon_reg.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    enddateTextBox.Text = dr["field1"].ToString();
                    bnkcodTextBox.Text = dr["field2"].ToString();


                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }


            try
            {



                enddateTextBox.Text = System.DateTime.Today.ToShortDateString();


                enddateTextBox.Text = System.DateTime.Now.Date.ToShortDateString();
            }
            catch { }

        }
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        voucherpay val = new voucherpay();

        bool check = val.ValidateReconciliationRegister(UserLogin.UserName, "vailidate", enddateTextBox.Text.Trim(), bnkcodTextBox.Text.Trim(), "");

        string value = Convert.ToString(check);
        if (value == "True")
        {

            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource1.SelectParameters["prmrecreg"].DefaultValue = "recreg";
            ObjectDataSource1.SelectParameters["prmendDate"].DefaultValue = enddateTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmbnkcod"].DefaultValue = bnkcodTextBox.Text.Trim();
            ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = Convert.ToString(HiddenFieldPost.Value);



            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'bnk_recon_reg.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                if (ds.Tables[0].Rows.Count == 0)
                {
                    SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2) values ('" + UserLogin.UserName + "','bnk_recon_reg.aspx' , '" + enddateTextBox.Text.Trim() + "', '" + bnkcodTextBox.Text.Trim() + "')", conn);
                    cmd_insert.ExecuteNonQuery();
                }
                else
                {
                    SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + enddateTextBox.Text.Trim() + "', field2 = '" + bnkcodTextBox.Text.Trim() + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'bnk_recon_reg.aspx' ", conn);
                    cmd_update.ExecuteNonQuery();
                }
                conn.Close();
            }
            catch (Exception ex)
            {
                Label1.Text = "Error :" + ex.Message + "<p>";
                conn.Close();
            }
            finally
            {
                conn.Close();
            }


            try
            {
                OutPutFile.Visible = true;
                HyperLink1.Visible = true;
                Label vpath = (Label)FormView1.FindControl("recregLabel");
                HyperLink1.Text = vpath.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


                if (vpath.Text == "")
                {
                    OutPutFile.Text = "No File Exists";

                }
            }
            catch { }

        }
    }   
}