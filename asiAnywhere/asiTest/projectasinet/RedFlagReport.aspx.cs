
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

public partial class RedFlagReport : System.Web.UI.Page
{



    protected void Page_Load(object sender, System.EventArgs e)
    {


        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "RedFlagReport.aspx";
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
                    Response.Write("hello");
                    SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
                    try
                    {
                        conn.Open();

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'RedFlagReport.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            BegCustomers_PartNo_TextBox.Text = dr["field1"].ToString();
                            BegSuppliers_FGitems_TextBox.Text = dr["field2"].ToString();
                            BegVendorCode_TextBox.Text = dr["field3"].ToString();
                            BegVendorPlantCode_TextBox.Text = dr["field4"].ToString();
                            EndCustomers_PartNo_TextBox.Text = dr["field5"].ToString();
                            EndSuppliers_FGitems_TextBox.Text = dr["field6"].ToString();
                            EndVendorCode_TextBox.Text = dr["field7"].ToString();
                            EndVendorPlantCode_TextBox.Text = dr["field8"].ToString();
                            ProductToStock_Text.Text = dr["field9"].ToString();

                            
                            if (dr["chk_field1"].ToString() == "Yes")
                                PrintMatReqCheckBox.Checked = true;
                            else
                                PrintMatReqCheckBox.Checked = false;
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

                    if (ProductToStock_Text.Text == "")
                        ProductToStock_Text.Text = "20000";
                    if (EndVendorCode_TextBox.Text == "")
                        EndVendorCode_TextBox.Text = "zzzzzzzz";
                    if (EndVendorPlantCode_TextBox.Text == "")
                        EndVendorPlantCode_TextBox.Text = "zzzzzzzz";
                    if (EndSuppliers_FGitems_TextBox.Text == "")
                        EndSuppliers_FGitems_TextBox.Text = "zzzzzzzzzzzzzzzz";
                    if (EndCustomers_PartNo_TextBox.Text == "")
                        EndCustomers_PartNo_TextBox.Text = "zzzzzzzzzzzzz";
                                        

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

                        string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'RedFlagReport.aspx'  ";
                        SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                        DataSet ds = new DataSet();
                        da.Fill(ds);
                        foreach (DataRow dr in ds.Tables[0].Rows)
                        {
                            BegCustomers_PartNo_TextBox.Text = dr["field1"].ToString();
                            BegSuppliers_FGitems_TextBox.Text = dr["field2"].ToString();
                            BegVendorCode_TextBox.Text = dr["field3"].ToString();
                            BegVendorPlantCode_TextBox.Text = dr["field4"].ToString();
                            EndCustomers_PartNo_TextBox.Text = dr["field5"].ToString();
                            EndSuppliers_FGitems_TextBox.Text = dr["field6"].ToString();
                            EndVendorCode_TextBox.Text = dr["field7"].ToString();
                            EndVendorPlantCode_TextBox.Text = dr["field8"].ToString();
                            ProductToStock_Text.Text = dr["field9"].ToString();

                            if (dr["chk_field1"].ToString() == "Yes")
                                PrintMatReqCheckBox.Checked = true;
                            else
                                PrintMatReqCheckBox.Checked = false;
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
                    if (ProductToStock_Text.Text == "")
                        ProductToStock_Text.Text = "20000";
                    if (EndVendorCode_TextBox.Text == "")
                        EndVendorCode_TextBox.Text = "zzzzzzzz";
                    if (EndVendorPlantCode_TextBox.Text == "")
                        EndVendorPlantCode_TextBox.Text = "zzzzzzzz";
                    if (EndSuppliers_FGitems_TextBox.Text == "")
                        EndSuppliers_FGitems_TextBox.Text = "zzzzzzzzzzzzzzzz";
                    if (EndCustomers_PartNo_TextBox.Text == "")
                        EndCustomers_PartNo_TextBox.Text = "zzzzzzzzzzzzz";
                   

                }

            }

        //    if (vCanRun == false)
        //    {
        //        Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
        //        Response.Write("<script>window.location.href = 'login.aspx';</script>");

        //    }
        }

        //string sCulture = ConfigurationManager.AppSettings["LCID"];
        //if (!String.IsNullOrEmpty(sCulture))
        //{
        //    int nCulture = int.Parse(sCulture);
        //    System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        //}

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
        if (PrintMatReqCheckBox.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
        {
            HiddenField1.Value = "No";
        }
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Alpha";
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = "Yes";
        ObjectDataSource1.SelectParameters["vFIBegCustPartNo"].DefaultValue = BegCustomers_PartNo_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vFIBegFgItemNo"].DefaultValue = BegSuppliers_FGitems_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vFIBegVendCode"].DefaultValue = BegVendorCode_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vFIBegVendPlantCode"].DefaultValue = BegVendorPlantCode_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vFIEndCustPartNo"].DefaultValue = EndCustomers_PartNo_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vFIEndFgItemNo"].DefaultValue = EndSuppliers_FGitems_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vFIEndVendCode"].DefaultValue = EndVendorCode_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vFIEndVendPlantCode"].DefaultValue = EndVendorPlantCode_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["vFINumberOfWeeks"].DefaultValue = ProductToStock_Text.Text.Trim();

        ObjectDataSource1.SelectParameters["vTGPrintRqMaterials"].DefaultValue = HiddenField1.Value.Trim();


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'RedFlagReport.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name, field1, field2, field3, field4, field5, field6, field7, field8, field9, chk_field1) values ('" + UserLogin.UserName + "','RedFlagReport.aspx','" + BegCustomers_PartNo_TextBox.Text.Trim() + "','" + BegSuppliers_FGitems_TextBox.Text.Trim() + "','" + BegVendorCode_TextBox.Text.Trim() + "','" + BegVendorPlantCode_TextBox.Text.Trim() + "','" + EndCustomers_PartNo_TextBox.Text.Trim() + "','" + EndSuppliers_FGitems_TextBox.Text.Trim() + "','" + EndVendorCode_TextBox.Text.Trim() + "','" + EndVendorPlantCode_TextBox.Text.Trim() + "','" + ProductToStock_Text.Text.Trim() + "','" + HiddenField1.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + BegCustomers_PartNo_TextBox.Text.Trim() + "', field2 = '" + BegSuppliers_FGitems_TextBox.Text.Trim() + "', field3 = '" + BegVendorCode_TextBox.Text.Trim() + "', field4 = '" + BegVendorPlantCode_TextBox.Text.Trim() + "', field5 = '" + EndCustomers_PartNo_TextBox.Text.Trim() + "', field6 = '" + EndSuppliers_FGitems_TextBox.Text.Trim() + "', field7 = '" + EndVendorCode_TextBox.Text.Trim() + "', field8 = '" + EndVendorPlantCode_TextBox.Text.Trim() + "', field9 = '" + ProductToStock_Text.Text.Trim() + "', chk_field1 = '" + HiddenField1.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name ='RedFlagReport.aspx' ", conn);
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



        //if (RadioButtonList8.SelectedIndex == 1)
        //{
            try
            {
                OutputLabel.Visible = true;
                HyperLink1.Visible = true;
                Label path = (Label)FormView1.FindControl("vRedFlagFileLabel");
                HyperLink1.Text = path.Text;
                HyperLink1.NavigateUrl = @"/pdfs/" + path.Text;

                if (path.Text == "")
                {
                    Label1.Text = "No Csv Exists";
                    Response.Write("<script>window.location.href='RedFlagReport.aspx'</script>");
                }
            }
            catch
            {

            }
        //}
        //if (RadioButtonList8.SelectedIndex == 0)
        //{

        //    try
        //    {
        //        Label vpath = (Label)FormView1.FindControl("vRedFlagFileLabel");

        //        if (vpath.Text != "")
        //        {
        //            string path = vpath.Text;
        //            string path2 = @"/pdfs/" + path;
        //            Session["open_red_flag"] = path2;
        //            if (path2 != "")
        //            {
        //                if (!Request.Browser.Browser.Contains("Safari"))
        //                    Response.Write("<script>window.open('print_alpha_list.aspx'); target='_blank'</script>");
        //                else
        //                    Response.Redirect("RedFlagReport.aspx");
        //            }
        //        }
        //        else
        //        {
        //            Label1.Text = "No Pdf Exists";
        //        }
        //    }
        //    catch
        //    {
        //        Label1.Text = "No Pdf Exists";
        //    }
        //    if (Label1.Text == "")
        //    {
        //        Response.Write("<script>window.location.href='RedFlagReport.aspx'</script>");
        //    }
        //}
    }

    protected void FormView1_PreRender(object sender, EventArgs e)
    {
        try
        {
            Label vpath = (Label)FormView1.FindControl("vRedFlagFileLabel");

            if (vpath.Text != "")
            {
                string path = vpath.Text;
                string path2 = @"/pdfs/" + path;
                Session["open_red_flag"] = path2;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>window.open('print_red_flag_report.aspx'); target='_blank'</script>");
                    else
                        Response.Redirect("RedFlagReport.aspx");
                }
            }
            else
            {
                Label1.Text = "No Pdf Exists";
            }
        }
        catch
        {
            Label1.Text = "No Pdf Exists";
        }
        if (Label1.Text == "")
        {
            Response.Write("<script>window.location.href='RedFlagReport.aspx'</script>");
        }
    }
}
