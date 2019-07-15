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

public partial class machine_file : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "machine_file_report.aspx";
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'machine_file_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    BegMachTextBox.Text = dr["field1"].ToString();
                    EndMachTextBox.Text = dr["field2"].ToString();
                    BegDeptTextBox.Text = dr["field3"].ToString();
                    EndDeptTextBox.Text = dr["field4"].ToString();

                    if (dr["chk_field1"].ToString() == "Yes")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;
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
                if (EndMachTextBox.Text == "")
                    EndMachTextBox.Text = "zzzzz";
                if (EndDeptTextBox.Text == "")
                    EndDeptTextBox.Text = "zz";                

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

        if (CheckBox1.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "MachineFileRep";
        ObjectDataSource1.SelectParameters["prmBegMachine"].DefaultValue = BegMachTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndMachine"].DefaultValue = EndMachTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBegDeptCode"].DefaultValue = BegDeptTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEndDeptCode"].DefaultValue = EndDeptTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmShowStandard"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList_out.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'machine_file_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, chk_field1) values ('" + UserLogin.UserName + "','machine_file_report.aspx' , '" + BegMachTextBox.Text.Trim() + "', '" + EndMachTextBox.Text.Trim() + "', '" + BegDeptTextBox.Text.Trim() + "', '" + EndDeptTextBox.Text.Trim() + "', '" + HiddenField1.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + BegMachTextBox.Text.Trim() + "', field2 = '" + EndMachTextBox.Text.Trim() + "', field3 = '" + BegDeptTextBox.Text.Trim() + "', field4 = '" + EndDeptTextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'machine_file_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("vFileLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                //Response.Write("<script>window.location.href='machine_file_report.aspx'</script>");
            }
        }
        catch { }       

    }

   }
