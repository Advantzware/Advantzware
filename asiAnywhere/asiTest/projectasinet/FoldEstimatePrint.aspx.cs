
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

public partial class FoldEstimatePrint : System.Web.UI.Page
{

    protected void Page_Load(object sender, System.EventArgs e)
    {


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "FoldEstimatePrint.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Session["alpha_login"] = PrmComp;
            if (aUsers == "external")
            {
                if (!Page.IsPostBack)
                {                
                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'FoldEstimatePrint.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            FrmDept_TextBox.Text = dr["field1"].ToString();
                            ToDept_TextBox.Text = dr["field2"].ToString();                            
                            
                            if (dr["chk_field1"].ToString() == "Yes")
                                PrintMftDeptNoteCheckBox.Checked = true;
                            else
                                PrintMftDeptNoteCheckBox.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                PrintBoxDsgCheckBox.Checked = true;
                            else
                                PrintBoxDsgCheckBox.Checked = false;
                        }
                        
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }



                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"];
                        lblUser.Text = UserLogin.UserName;
                        Session["appha_user_login"] = lblUser.Text;
                    }
                   
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;


                    if (ToDept_TextBox.Text == "")
                        ToDept_TextBox.Text = "zzzzzzzzzzzz";                                                           

                }
            }
            if (aUsers == "internal")
            {
                if (!Page.IsPostBack)
                {
                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'FoldEstimatePrint.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            FrmDept_TextBox.Text = dr["field1"].ToString();
                            ToDept_TextBox.Text = dr["field2"].ToString();

                            if (dr["chk_field1"].ToString() == "Yes")
                                PrintMftDeptNoteCheckBox.Checked = true;
                            else
                                PrintMftDeptNoteCheckBox.Checked = false;

                            if (dr["chk_field2"].ToString() == "Yes")
                                PrintBoxDsgCheckBox.Checked = true;
                            else
                                PrintBoxDsgCheckBox.Checked = false;
                        }                        
                    }
                    catch
                    {
                        conn.Close();
                    }
                    finally
                    {
                        conn.Close();
                    }


                    if (Session["User"] != null)
                    {
                        //UserClass UserLogin = (UserClass)Session["User"];
                        lblUser.Text = UserLogin.UserName;
                        Session["appha_user_login"] = lblUser.Text;
                    }
                    OutputLabel.Visible = false;
                    HyperLink1.Visible = false;
                    
                    if (ToDept_TextBox.Text == "")
                        ToDept_TextBox.Text = "zzzzzzzzzzzz";  
                   
                }

            }

        
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
   
    protected void SubmitButton_Click(object sender, EventArgs e)
    {                
        if (PrintMftDeptNoteCheckBox.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        if (PrintBoxDsgCheckBox.Checked)
        {
            HiddenField2.Value = "Yes";
        }
        else
        {
            HiddenField2.Value = "No";
        }
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Alpha";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = "Yes";
        ObjectDataSource1.SelectParameters["vFromDept"].DefaultValue = FrmDept_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vToDept"].DefaultValue = ToDept_TextBox.Text.Trim();              
        ObjectDataSource1.SelectParameters["vTbPrtNote"].DefaultValue = HiddenField1.Value.Trim();
        ObjectDataSource1.SelectParameters["vTbPrtBox"].DefaultValue = HiddenField2.Value.Trim();

        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_folding_est"]);
        ObjectDataSource1.SelectParameters["prmFormNo"].DefaultValue = Convert.ToString(Session["order_folding_formno"]);
        ObjectDataSource1.SelectParameters["prmBlankNo"].DefaultValue = Convert.ToString(Session["order_folding_blankno"]);
        ObjectDataSource1.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["fold_est_line"]);


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'FoldEstimatePrint.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, chk_field1, chk_field2) values ('" + UserLogin.UserName + "','FoldEstimatePrint.aspx','" + FrmDept_TextBox.Text.Trim() + "','" + ToDept_TextBox.Text.Trim() + "','" + HiddenField1.Value + "','" + HiddenField2.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + FrmDept_TextBox.Text.Trim() + "', field2 = '" + ToDept_TextBox.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "', chk_field2 = '" + HiddenField2.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='FoldEstimatePrint.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }

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
                OutputLabel.Visible = true;
                HyperLink1.Visible = true;
                Label path = (Label)FormView1.FindControl("vCorrEstimateFileLabel");
                HyperLink1.Text = path.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;
                string path2 = @"/pdfs/" + path.Text;

                if (path.Text != "")
                {                    
                    Session["corr_est_list_printtext"] = path2;
                    if (path2 != "")
                    {
                        if (!Request.Browser.Browser.Contains("Safari"))
                            Response.Write("<script>window.open('print_corr_text.aspx'); target='_blank'</script>");
                        else
                            Response.Redirect("FoldEstimatePrint.aspx");
                    }
                }
                else
                {
                    //Label1.Text = "No Pdf Exists";
                }
            }
            catch
            {
                //Label1.Text = "No Pdf Exists";
            }
    }

    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        
    }
}
