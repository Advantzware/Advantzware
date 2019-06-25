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

public partial class quoted_price : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "quoted_price_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;

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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'quoted_price_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    BegCustTextBox.Text     = dr["field1"].ToString();
                    EndCustTextBox.Text     = dr["field2"].ToString();
                    BegSalrepTextBox.Text   = dr["field3"].ToString();
                    EndSalrepTextBox.Text   = dr["field4"].ToString();
                    BegdateTextBox.Text     = dr["field5"].ToString();
                    EnddateTextBox.Text     = dr["field6"].ToString();              
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
                if (EndCustTextBox.Text == "")
                    EndCustTextBox.Text = "zzzzzzzz";
                if (EnddateTextBox.Text == "")
                    EnddateTextBox.Text = "12/31/9999";
                if (EndSalrepTextBox.Text == "")
                    EndSalrepTextBox.Text = "zzz";
          
                RadioButtonList_out.SelectedIndex = 0;
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

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "QuotedPriceRep";
        ObjectDataSource1.SelectParameters["prmBegCustomer"].DefaultValue = BegCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndCustomer"].DefaultValue = EndCustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBeginDate"].DefaultValue = BegdateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndDate"].DefaultValue = EnddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegSalrep"].DefaultValue = BegSalrepTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndSalrep"].DefaultValue = EndSalrepTextBox.Text.Trim();   
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'quoted_price_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6) values ('" + UserLogin.UserName + "','quoted_price_report.aspx' , '" + BegCustTextBox.Text.Trim() + "', '" + EndCustTextBox.Text.Trim() + "', '" + BegSalrepTextBox.Text.Trim() + "', '" + EndSalrepTextBox.Text.Trim() + "', '" + BegdateTextBox.Text.Trim() + "', '" + EnddateTextBox.Text.Trim() + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + BegCustTextBox.Text.Trim() + "', field2 = '" + EndCustTextBox.Text.Trim() + "', field3 = '" + BegSalrepTextBox.Text.Trim() + "', field4 = '" + EndSalrepTextBox.Text.Trim() + "', field5 = '" + BegdateTextBox.Text.Trim() + "', field6 = '" + EnddateTextBox.Text.Trim() + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'quoted_price_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("qPrcFileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                //Response.Write("<script>window.location.href='quoted_price_report.aspx'</script>");
            }
        }
        catch { }       

    }

   }
